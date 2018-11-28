use llvm_sys::prelude::*;
use llvm_sys as llvm;
use std::ffi::CString;
use std::ptr;
use ::libc::{c_uint, c_ulonglong, c_double};

// Don't implement Clone or Copy for any wrapper that also implements Drop.
#[derive(Copy, Clone, Debug, PartialEq)] pub struct Context(pub LLVMContextRef);
#[derive(Copy, Clone, Debug, PartialEq)] pub struct Builder(pub LLVMBuilderRef);
#[derive(Copy, Clone, Debug, PartialEq)] pub struct Module(pub LLVMModuleRef);
#[derive(Copy, Clone, Debug, PartialEq)] pub struct Type(pub LLVMTypeRef);
#[derive(Copy, Clone, Debug, PartialEq)] pub struct Value(pub LLVMValueRef);
#[derive(Copy, Clone, Debug, PartialEq)] pub struct BB(pub LLVMBasicBlockRef);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IntPredicate {
    EQ, NE,
    UnsignedGT, UnsignedGE, UnsignedLT, UnsignedLE,
    SignedGT, SignedGE, SignedLT, SignedLE,
}

impl IntPredicate {
    fn to_internal(&self) -> llvm::LLVMIntPredicate {
        match self {
            IntPredicate::EQ  => llvm::LLVMIntPredicate::LLVMIntEQ,
            IntPredicate::NE  => llvm::LLVMIntPredicate::LLVMIntNE,
            IntPredicate::UnsignedGT => llvm::LLVMIntPredicate::LLVMIntUGT,
            IntPredicate::UnsignedGE => llvm::LLVMIntPredicate::LLVMIntUGE,
            IntPredicate::UnsignedLT => llvm::LLVMIntPredicate::LLVMIntULT,
            IntPredicate::UnsignedLE => llvm::LLVMIntPredicate::LLVMIntULE,
            IntPredicate::SignedGT => llvm::LLVMIntPredicate::LLVMIntSGT,
            IntPredicate::SignedGE => llvm::LLVMIntPredicate::LLVMIntSGE,
            IntPredicate::SignedLT => llvm::LLVMIntPredicate::LLVMIntSLT,
            IntPredicate::SignedLE => llvm::LLVMIntPredicate::LLVMIntSLE,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RealPredicate {
    False, True,
    Ordered, Unordered,
    OrderedEQ, OrderedGT, OrderedGE, OrderedLT, OrderedLE, OrderedNE,
    UnorderedEQ, UnorderedGT, UnorderedGE, UnorderedLT, UnorderedLE, UnorderedNE,
}

impl RealPredicate {
    fn to_internal(&self) -> llvm::LLVMRealPredicate {
        match self {
            RealPredicate::False => llvm::LLVMRealPredicate::LLVMRealPredicateFalse,
            RealPredicate::OrderedEQ => llvm::LLVMRealPredicate::LLVMRealOEQ,
            RealPredicate::OrderedGT => llvm::LLVMRealPredicate::LLVMRealOGT,
            RealPredicate::OrderedGE => llvm::LLVMRealPredicate::LLVMRealOGE,
            RealPredicate::OrderedLT => llvm::LLVMRealPredicate::LLVMRealOLT,
            RealPredicate::OrderedLE => llvm::LLVMRealPredicate::LLVMRealOLE,
            RealPredicate::OrderedNE => llvm::LLVMRealPredicate::LLVMRealONE,
            RealPredicate::Ordered => llvm::LLVMRealPredicate::LLVMRealORD,
            RealPredicate::Unordered => llvm::LLVMRealPredicate::LLVMRealUNO,
            RealPredicate::UnorderedEQ => llvm::LLVMRealPredicate::LLVMRealUEQ,
            RealPredicate::UnorderedGT => llvm::LLVMRealPredicate::LLVMRealUGT,
            RealPredicate::UnorderedGE => llvm::LLVMRealPredicate::LLVMRealUGE,
            RealPredicate::UnorderedLT => llvm::LLVMRealPredicate::LLVMRealULT,
            RealPredicate::UnorderedLE => llvm::LLVMRealPredicate::LLVMRealULE,
            RealPredicate::UnorderedNE => llvm::LLVMRealPredicate::LLVMRealUNE,
            RealPredicate::True => llvm::LLVMRealPredicate::LLVMRealPredicateTrue,
        }
    }
}

macro_rules! c_bool {
    ($e: expr) => {
        if $e { 1 } else { 0 }
    }
}

impl Value {
    pub fn i1(v: bool) -> Value {
        Value(
            unsafe { llvm::core::LLVMConstInt(Type::i1().0, c_bool!(v), c_bool!(true)) }
        )
    }
    pub fn i32(v: i32) -> Value {
        Value(
            unsafe { llvm::core::LLVMConstInt(Type::i32().0, v as c_ulonglong, c_bool!(true)) }
        )
    }
    pub fn i64(v: i64) -> Value {
        Value(
            unsafe { llvm::core::LLVMConstInt(Type::i64().0, v as c_ulonglong, c_bool!(true)) }
        )
    }
    pub fn float(v: f32) -> Value {
        Value(
            unsafe { llvm::core::LLVMConstReal(Type::float().0, v as c_double) }
        )
    }
    pub fn double(v: f64) -> Value {
        Value(
            unsafe { llvm::core::LLVMConstReal(Type::double().0, v as c_double) }
        )
    }

    pub fn get_param(&self, i: usize) -> Value {
        Value(
            unsafe { llvm::core::LLVMGetParam(self.0, i as c_uint) }
        )
    }

    pub fn dump(&self) {
        unsafe {
            llvm::core::LLVMDumpValue(self.0)
        }
    }
}

impl Type {
    pub fn dump(&self) {
        unsafe {
            llvm::core::LLVMDumpType(self.0)
        }
    }

    pub fn i1() -> Type {
        Type(unsafe { llvm::core::LLVMInt1Type() })
    }

    pub fn i32() -> Type {
        Type(unsafe { llvm::core::LLVMInt32Type() })
    }

    pub fn i64() -> Type {
        Type(unsafe { llvm::core::LLVMInt64Type() })
    }
    pub fn float() -> Type {
        Type(unsafe { llvm::core::LLVMFloatType() })
    }

    pub fn double() -> Type {
        Type(unsafe { llvm::core::LLVMDoubleType() })
    }

    pub fn void() -> Type {
        Type(unsafe { llvm::core::LLVMVoidType() })
    }

    pub fn function(ret: Type, param_types: &[Type], is_var_arg: bool) -> Type {
        let n = param_types.len() as c_uint;
        let mut tys: Vec<LLVMTypeRef> = param_types.iter().map(|ty| ty.0).collect();
        Type(unsafe { llvm::core::LLVMFunctionType(ret.0, tys.as_mut_ptr(), n, c_bool!(is_var_arg)) })
    }

    pub fn array(element_type: Type, n: usize) -> Type {
        Type(unsafe { llvm::core::LLVMArrayType(element_type.0, n as c_uint) })
    }

    pub fn pointer(ty: Type) -> Type {
        Type(unsafe { llvm::core::LLVMPointerType(ty.0, 0) })
    }

    pub fn structure(element_types: &[Type], is_packed: bool) -> Type {
        let n = element_types.len() as c_uint;
        let mut tys: Vec<LLVMTypeRef> = element_types.iter().map(|ty| ty.0).collect();
        Type(unsafe { llvm::core::LLVMStructType(tys.as_mut_ptr(), n, c_bool!(is_packed)) })
    }
}

impl Module {
    pub fn new(name: &str) -> Module {
        let cstr = CString::new(name).unwrap();
        Module(unsafe { llvm::core::LLVMModuleCreateWithName(cstr.as_ptr()) })
    }

    pub fn dispose(&self) {
        unsafe {
            llvm::core::LLVMDisposeModule(self.0);
        }
    }

    pub fn add_function(&self, name: &str, ty: Type) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe { llvm::core::LLVMAddFunction(self.0, cstr.as_ptr(), ty.0) })
    }

    pub fn dump(&self) {
        unsafe {
            llvm::core::LLVMDumpModule(self.0)
        }
    }

    pub fn write_bitcode_to_file(&self, file: &str) {
        let cstr = CString::new(file).unwrap();
        unsafe {
            llvm::bit_writer::LLVMWriteBitcodeToFile(self.0, cstr.as_ptr());
        }
    }

    pub fn get_named_global(&self, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMGetNamedGlobal(self.0, cstr.as_ptr())
        })
    }

    pub fn get_named_function(&self, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMGetNamedFunction(self.0, cstr.as_ptr())
        })
    }

}

impl Context {
    pub fn new() -> Context {
        Context(unsafe { llvm::core::LLVMContextCreate() })
    }
    pub fn dispose(&self) {
        unsafe {
            llvm::core::LLVMContextDispose(self.0);
        }
    }
    pub fn new_builder(&self) -> Builder {
        Builder(unsafe { llvm::core::LLVMCreateBuilderInContext(self.0) })
    }
    pub fn void_type(&self) -> Type {
        Type(unsafe { llvm::core::LLVMVoidTypeInContext(self.0) })
    }
    pub fn label_type(&self) -> Type {
        Type(unsafe { llvm::core::LLVMLabelTypeInContext(self.0) })
    }
    pub fn token_type(&self) -> Type {
        Type(unsafe { llvm::core::LLVMTokenTypeInContext(self.0) })
    }
    pub fn metadata_type(&self) -> Type {
        Type(unsafe { llvm::core::LLVMMetadataTypeInContext(self.0) })
    }

    pub fn i1_type(&self, ) -> Type {
        Type(unsafe { llvm::core::LLVMInt1TypeInContext(self.0) })
    }

    pub fn i32_type(&self, ) -> Type {
        Type(unsafe { llvm::core::LLVMInt32TypeInContext(self.0) })
    }

    pub fn i64_type(&self, ) -> Type {
        Type(unsafe { llvm::core::LLVMInt64TypeInContext(self.0) })
    }
    pub fn float_type(&self, ) -> Type {
        Type(unsafe { llvm::core::LLVMFloatTypeInContext(self.0) })
    }

    pub fn double_type(&self, ) -> Type {
        Type(unsafe { llvm::core::LLVMDoubleTypeInContext(self.0) })
    }

    pub fn function_type(&self, ret: Type, param_types: &[Type], is_var_arg: bool) -> Type {
        let n = param_types.len() as c_uint;
        let mut tys: Vec<LLVMTypeRef> = param_types.iter().map(|ty| ty.0).collect();
        Type(unsafe { llvm::core::LLVMFunctionType(ret.0, tys.as_mut_ptr(), n, c_bool!(is_var_arg)) })
    }

    pub fn array_type(&self, element_type: Type, n: usize) -> Type {
        Type(unsafe { llvm::core::LLVMArrayType(element_type.0, n as c_uint) })
    }

    pub fn pointer_type(&self, ty: Type) -> Type {
        Type(unsafe { llvm::core::LLVMPointerType(ty.0, 0) })
    }

    pub fn structure_type(&self, element_types: &[Type], is_packed: bool) -> Type {
        let n = element_types.len() as c_uint;
        let mut tys: Vec<LLVMTypeRef> = element_types.iter().map(|ty| ty.0).collect();
        Type(unsafe { llvm::core::LLVMStructTypeInContext(self.0, tys.as_mut_ptr(), n, c_bool!(is_packed)) })
    }

    pub fn append_bb(&self, function: Value, name: &str) -> BB {
        let cstr = CString::new(name).unwrap();
        BB(unsafe {
            llvm::core::LLVMAppendBasicBlockInContext(self.0, function.0, cstr.as_ptr())
        })
    }
}

impl Builder {
    pub fn new() -> Builder {
        Builder(unsafe { llvm::core::LLVMCreateBuilder() })
    }

    pub fn dispose(&self) {
        unsafe {
            llvm::core::LLVMDisposeBuilder(self.0);
        }
    }

    pub fn position(&self, bb: BB, insn: Value) {
        unsafe {
            llvm::core::LLVMPositionBuilder(self.0, bb.0, insn.0)
        }
    }
    pub fn position_before(&self, insn: Value) {
        unsafe {
            llvm::core::LLVMPositionBuilderBefore(self.0, insn.0)
        }
    }

    pub fn position_at_end(&self, bb: BB) {
        unsafe {
            llvm::core::LLVMPositionBuilderAtEnd(self.0, bb.0)
        }
    }

    pub fn get_insert_block(&self) -> BB {
        BB(unsafe {
            llvm::core::LLVMGetInsertBlock(self.0)
        })
    }

    pub fn clear_insertion_position(&self) {
        unsafe {
            llvm::core::LLVMClearInsertionPosition(self.0)
        }
    }

    pub fn insert(&self, insn: Value) {
        unsafe {
            llvm::core::LLVMInsertIntoBuilder(self.0, insn.0)
        }
    }

    pub fn insert_with_name(&self, insn: Value, name: &str) {
        let cstr = CString::new(name).unwrap();
        unsafe {
            llvm::core::LLVMInsertIntoBuilderWithName(self.0, insn.0, cstr.as_ptr())
        }
    }

    pub fn set_current_debug_location(&self, l: Value) {
        unsafe {
            llvm::core::LLVMSetCurrentDebugLocation(self.0, l.0)
        }
    }

    pub fn get_current_debug_location(&self) -> Value {
        Value(unsafe {
            llvm::core::LLVMGetCurrentDebugLocation(self.0)
        })
    }

    pub fn set_inst_debug_location(&self, l: Value) {
        unsafe {
            llvm::core::LLVMSetInstDebugLocation(self.0, l.0)
        }
    }

    pub fn ret_void(&self) -> Value {
        Value(unsafe {
            llvm::core::LLVMBuildRetVoid(self.0)
        })
    }

    pub fn ret(&self, v: Value) -> Value {
        Value(unsafe {
            llvm::core::LLVMBuildRet(self.0, v.0)
        })
    }

    pub fn aggregate_ret(&self, vs: &[Value]) -> Value {
        let mut a: Vec<LLVMValueRef> = vs.iter().map(|v| v.0).collect();
        Value(unsafe {
            llvm::core::LLVMBuildAggregateRet(self.0, a.as_mut_ptr(), vs.len() as u32)
        })
    }

    pub fn br(&self, dest: BB) -> Value {
        Value(unsafe {
            llvm::core::LLVMBuildBr(self.0, dest.0)
        })
    }

    pub fn cond_br(&self, i: Value, t: BB, e: BB) -> Value {
        Value(unsafe {
            llvm::core::LLVMBuildCondBr(self.0, i.0, t.0, e.0)
        })
    }

    pub fn switch(&self, i: Value, e: BB, ncases: usize) -> Value {
        Value(unsafe {
            llvm::core::LLVMBuildSwitch(self.0, i.0, e.0, ncases as u32)
        })
    }

// LLVMValueRef 	LLVMBuildSwitch (LLVMBuilderRef, LLVMValueRef V, LLVMBasicBlockRef Else, unsigned NumCases)
// LLVMValueRef 	LLVMBuildIndirectBr (LLVMBuilderRef B, LLVMValueRef Addr, unsigned NumDests)
// LLVMValueRef 	LLVMBuildInvoke (LLVMBuilderRef, LLVMValueRef Fn, LLVMValueRef *Args, unsigned NumArgs, LLVMBasicBlockRef Then, LLVMBasicBlockRef Catch, const char *Name)

    pub fn unreachable(&self) -> Value {
        Value(unsafe {
            llvm::core::LLVMBuildUnreachable(self.0)
        })
    }

// LLVMValueRef 	LLVMBuildResume (LLVMBuilderRef B, LLVMValueRef Exn)
// LLVMValueRef 	LLVMBuildLandingPad (LLVMBuilderRef B, LLVMTypeRef Ty, LLVMValueRef PersFn, unsigned NumClauses, const char *Name)
// LLVMValueRef 	LLVMBuildCleanupRet (LLVMBuilderRef B, LLVMValueRef CatchPad, LLVMBasicBlockRef BB)
// LLVMValueRef 	LLVMBuildCatchRet (LLVMBuilderRef B, LLVMValueRef CatchPad, LLVMBasicBlockRef BB)
// LLVMValueRef 	LLVMBuildCatchPad (LLVMBuilderRef B, LLVMValueRef ParentPad, LLVMValueRef *Args, unsigned NumArgs, const char *Name)
// LLVMValueRef 	LLVMBuildCleanupPad (LLVMBuilderRef B, LLVMValueRef ParentPad, LLVMValueRef *Args, unsigned NumArgs, const char *Name)
// LLVMValueRef 	LLVMBuildCatchSwitch (LLVMBuilderRef B, LLVMValueRef ParentPad, LLVMBasicBlockRef UnwindBB, unsigned NumHandlers, const char *Name)

    pub fn add_case(&self, switch: Value, on_val: Value, dest: BB) {
        unsafe {
            llvm::core::LLVMAddCase(switch.0, on_val.0, dest.0)
        }
    }
//
// void 	LLVMAddDestination (LLVMValueRef IndirectBr, LLVMBasicBlockRef Dest)
// unsigned 	LLVMGetNumClauses (LLVMValueRef LandingPad)
// LLVMValueRef 	LLVMGetClause (LLVMValueRef LandingPad, unsigned Idx)
// void 	LLVMAddClause (LLVMValueRef LandingPad, LLVMValueRef ClauseVal)
// LLVMBool 	LLVMIsCleanup (LLVMValueRef LandingPad)
// void 	LLVMSetCleanup (LLVMValueRef LandingPad, LLVMBool Val)
// void 	LLVMAddHandler (LLVMValueRef CatchSwitch, LLVMBasicBlockRef Dest)
// unsigned 	LLVMGetNumHandlers (LLVMValueRef CatchSwitch)
// void 	LLVMGetHandlers (LLVMValueRef CatchSwitch, LLVMBasicBlockRef *Handlers)
//  	Obtain the basic blocks acting as handlers for a catchswitch instruction. More...
// LLVMValueRef 	LLVMGetArgOperand (LLVMValueRef Funclet, unsigned i)
// void 	LLVMSetArgOperand (LLVMValueRef Funclet, unsigned i, LLVMValueRef value)
// LLVMValueRef 	LLVMGetParentCatchSwitch (LLVMValueRef CatchPad)
//  	Get the parent catchswitch instruction of a catchpad instruction. More...
// void 	LLVMSetParentCatchSwitch (LLVMValueRef CatchPad, LLVMValueRef CatchSwitch)
//  	Set the parent catchswitch instruction of a catchpad instruction. More...

    pub fn add(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildAdd(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn nsw_add(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildNSWAdd(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn nuw_add(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildNUWAdd(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn fadd(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildFAdd(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn sub(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildSub(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn nsw_sub(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildNSWSub(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn nuw_sub(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildNUWSub(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn fsub(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildFSub(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn mul(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildMul(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn nsw_mul(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildNSWMul(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn nuw_mul(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildNUWMul(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn fmul(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildFMul(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn udiv(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildUDiv(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn exact_udiv(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildExactUDiv(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn sdiv(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildSDiv(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn exact_sdiv(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildExactSDiv(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn fdiv(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildFDiv(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn urem(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildURem(self.0, left.0, right.0, cstr.as_ptr())
        })
    }
    pub fn srem(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildSRem(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn frem(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildFRem(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn shl(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildShl(self.0, left.0, right.0, cstr.as_ptr())
        })
    }
    pub fn lshr(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildLShr(self.0, left.0, right.0, cstr.as_ptr())
        })
    }
    pub fn ashr(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildAShr(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn and(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildAnd(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn or(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildOr(self.0, left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn xor(&self, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildXor(self.0, left.0, right.0, cstr.as_ptr())
        })
    }
// LLVMValueRef 	LLVMBuildBinOp (LLVMBuilderRef B, LLVMOpcode Op, LLVMValueRef LHS, LLVMValueRef RHS, const char *Name)

    pub fn neg(&self, v: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildNeg(self.0, v.0, cstr.as_ptr())
        })
    }

    pub fn nsw_neg(&self, v: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildNSWNeg(self.0, v.0, cstr.as_ptr())
        })
    }
    pub fn nuw_neg(&self, v: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildNUWNeg(self.0, v.0, cstr.as_ptr())
        })
    }
    pub fn fneg(&self, v: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildFNeg(self.0, v.0, cstr.as_ptr())
        })
    }
    pub fn not(&self, v: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildNot(self.0, v.0, cstr.as_ptr())
        })
    }

// LLVMValueRef 	LLVMBuildMalloc (LLVMBuilderRef, LLVMTypeRef Ty, const char *Name)
// LLVMValueRef 	LLVMBuildArrayMalloc (LLVMBuilderRef, LLVMTypeRef Ty, LLVMValueRef Val, const char *Name)
// LLVMValueRef 	LLVMBuildMemSet (LLVMBuilderRef B, LLVMValueRef Ptr, LLVMValueRef Val, LLVMValueRef Len, unsigned Align)
//  	Creates and inserts a memset to the specified pointer and the specified value. More...
// LLVMValueRef 	LLVMBuildMemCpy (LLVMBuilderRef B, LLVMValueRef Dst, unsigned DstAlign, LLVMValueRef Src, unsigned SrcAlign, LLVMValueRef Size)
//  	Creates and inserts a memcpy between the specified pointers. More...
// LLVMValueRef 	LLVMBuildMemMove (LLVMBuilderRef B, LLVMValueRef Dst, unsigned DstAlign, LLVMValueRef Src, unsigned SrcAlign, LLVMValueRef Size)
//  	Creates and inserts a memmove between the specified pointers. More...

    pub fn alloca(&self, ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildAlloca(self.0, ty.0, cstr.as_ptr())
        })
    }

// LLVMValueRef 	LLVMBuildArrayAlloca (LLVMBuilderRef, LLVMTypeRef Ty, LLVMValueRef Val, const char *Name)
//
// LLVMValueRef 	LLVMBuildFree (LLVMBuilderRef, LLVMValueRef PointerVal)
//


    pub fn load(&self, ptr: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildLoad(self.0, ptr.0, cstr.as_ptr())
        })
    }

    pub fn store(&self, v: Value, ptr: Value) -> Value {
        Value(unsafe {
            llvm::core::LLVMBuildStore(self.0, v.0, ptr.0)
        })
    }

    pub fn get_element_pointer(&self, ptr: Value, indices: &[Value], name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        let mut is: Vec<LLVMValueRef> = indices.iter().map(|v| v.0).collect();
        Value(unsafe {
            llvm::core::LLVMBuildGEP(self.0, ptr.0, is.as_mut_ptr(), indices.len() as u32, cstr.as_ptr())
        })
    }

    pub fn get_in_bounds_element_pointer(&self, ptr: Value, indices: &[Value], name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        let mut is: Vec<LLVMValueRef> = indices.iter().map(|v| v.0).collect();
        Value(unsafe {
            llvm::core::LLVMBuildInBoundsGEP(self.0, ptr.0, is.as_mut_ptr(), indices.len() as u32, cstr.as_ptr())
        })
    }

    pub fn get_struct_element_pointer(&self, ptr: Value, index: usize, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildStructGEP(self.0, ptr.0, index as u32, cstr.as_ptr())
        })
    }

// LLVMValueRef 	LLVMBuildGlobalString (LLVMBuilderRef B, const char *Str, const char *Name)
// LLVMValueRef 	LLVMBuildGlobalStringPtr (LLVMBuilderRef B, const char *Str, const char *Name)
//
// LLVMBool 	LLVMGetVolatile (LLVMValueRef MemoryAccessInst)
// void 	LLVMSetVolatile (LLVMValueRef MemoryAccessInst, LLVMBool IsVolatile)
//
// LLVMAtomicOrdering 	LLVMGetOrdering (LLVMValueRef MemoryAccessInst)
// void 	LLVMSetOrdering (LLVMValueRef MemoryAccessInst, LLVMAtomicOrdering Ordering)
//

    pub fn trunc(&self, v: Value, dst_ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildTrunc(self.0, v.0, dst_ty.0, cstr.as_ptr())
        })
    }

    pub fn zext(&self, v: Value, dst_ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildZExt(self.0, v.0, dst_ty.0, cstr.as_ptr())
        })
    }

    pub fn sext(&self, v: Value, dst_ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildSExt(self.0, v.0, dst_ty.0, cstr.as_ptr())
        })
    }

    pub fn fp_to_ui(&self, v: Value, dst_ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildFPToUI(self.0, v.0, dst_ty.0, cstr.as_ptr())
        })
    }

    pub fn fp_to_si(&self, v: Value, dst_ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildFPToUI(self.0, v.0, dst_ty.0, cstr.as_ptr())
        })
    }

    pub fn ui_to_fp(&self, v: Value, dst_ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildUIToFP(self.0, v.0, dst_ty.0, cstr.as_ptr())
        })
    }

    pub fn si_to_fp(&self, v: Value, dst_ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildSIToFP(self.0, v.0, dst_ty.0, cstr.as_ptr())
        })
    }

    pub fn fptrunc(&self, v: Value, dst_ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildFPTrunc(self.0, v.0, dst_ty.0, cstr.as_ptr())
        })
    }

    pub fn fpext(&self, v: Value, dst_ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildFPExt(self.0, v.0, dst_ty.0, cstr.as_ptr())
        })
    }

    pub fn ptr_to_int(&self, v: Value, dst_ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildPtrToInt(self.0, v.0, dst_ty.0, cstr.as_ptr())
        })
    }
    pub fn int_to_ptr(&self, v: Value, dst_ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildIntToPtr(self.0, v.0, dst_ty.0, cstr.as_ptr())
        })
    }
    pub fn bitcast(&self, v: Value, dst_ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildBitCast(self.0, v.0, dst_ty.0, cstr.as_ptr())
        })
    }

// LLVMValueRef 	LLVMBuildBitCast (LLVMBuilderRef, LLVMValueRef Val, LLVMTypeRef DestTy, const char *Name)
// LLVMValueRef 	LLVMBuildAddrSpaceCast (LLVMBuilderRef, LLVMValueRef Val, LLVMTypeRef DestTy, const char *Name)
// LLVMValueRef 	LLVMBuildZExtOrBitCast (LLVMBuilderRef, LLVMValueRef Val, LLVMTypeRef DestTy, const char *Name)
// LLVMValueRef 	LLVMBuildSExtOrBitCast (LLVMBuilderRef, LLVMValueRef Val, LLVMTypeRef DestTy, const char *Name)
// LLVMValueRef 	LLVMBuildTruncOrBitCast (LLVMBuilderRef, LLVMValueRef Val, LLVMTypeRef DestTy, const char *Name)
// LLVMValueRef 	LLVMBuildCast (LLVMBuilderRef B, LLVMOpcode Op, LLVMValueRef Val, LLVMTypeRef DestTy, const char *Name)
// LLVMValueRef 	LLVMBuildPointerCast (LLVMBuilderRef, LLVMValueRef Val, LLVMTypeRef DestTy, const char *Name)
// LLVMValueRef 	LLVMBuildIntCast (LLVMBuilderRef, LLVMValueRef Val, LLVMTypeRef DestTy, const char *Name)
// LLVMValueRef 	LLVMBuildFPCast (LLVMBuilderRef, LLVMValueRef Val, LLVMTypeRef DestTy, const char *Name)


    pub fn icmp(&self, pred: IntPredicate, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildICmp(self.0, pred.to_internal(), left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn fcmp(&self, pred: RealPredicate, left: Value, right: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildFCmp(self.0, pred.to_internal(), left.0, right.0, cstr.as_ptr())
        })
    }

    pub fn phi(&self, ty: Type, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildPhi(self.0, ty.0, cstr.as_ptr())
        })
    }

    pub fn call(&self, fun: Value, argv: &[Value], name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        let mut vs: Vec<LLVMValueRef> = argv.iter().map(|v| v.0).collect();
        Value(unsafe {
            llvm::core::LLVMBuildCall(self.0, fun.0, vs.as_mut_ptr(), argv.len() as u32, cstr.as_ptr())
        })
    }

    pub fn select(&self, i: Value, t: Value, e: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildSelect(self.0, i.0, t.0, e.0, cstr.as_ptr())
        })
    }

// LLVMValueRef 	LLVMBuildVAArg (LLVMBuilderRef, LLVMValueRef List, LLVMTypeRef Ty, const char *Name)
//
// LLVMValueRef 	LLVMBuildExtractElement (LLVMBuilderRef, LLVMValueRef VecVal, LLVMValueRef Index, const char *Name)
//
// LLVMValueRef 	LLVMBuildInsertElement (LLVMBuilderRef, LLVMValueRef VecVal, LLVMValueRef EltVal, LLVMValueRef Index, const char *Name)
//
// LLVMValueRef 	LLVMBuildShuffleVector (LLVMBuilderRef, LLVMValueRef V1, LLVMValueRef V2, LLVMValueRef Mask, const char *Name)
//
// LLVMValueRef 	LLVMBuildExtractValue (LLVMBuilderRef, LLVMValueRef AggVal, unsigned Index, const char *Name)
//
// LLVMValueRef 	LLVMBuildInsertValue (LLVMBuilderRef, LLVMValueRef AggVal, LLVMValueRef EltVal, unsigned Index, const char *Name)

    pub fn is_null(&self, v: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildIsNull(self.0, v.0, cstr.as_ptr())
        })
    }

    pub fn is_not_null(&self, v: Value, name: &str) -> Value {
        let cstr = CString::new(name).unwrap();
        Value(unsafe {
            llvm::core::LLVMBuildIsNotNull(self.0, v.0, cstr.as_ptr())
        })
    }

// LLVMValueRef 	LLVMBuildPtrDiff (LLVMBuilderRef, LLVMValueRef LHS, LLVMValueRef RHS, const char *Name)
//
// LLVMValueRef 	LLVMBuildFence (LLVMBuilderRef B, LLVMAtomicOrdering ordering, LLVMBool singleThread, const char *Name)
//
// LLVMValueRef 	LLVMBuildAtomicRMW (LLVMBuilderRef B, LLVMAtomicRMWBinOp op, LLVMValueRef PTR, LLVMValueRef Val, LLVMAtomicOrdering ordering, LLVMBool singleThread)
//
// LLVMValueRef 	LLVMBuildAtomicCmpXchg (LLVMBuilderRef B, LLVMValueRef Ptr, LLVMValueRef Cmp, LLVMValueRef New, LLVMAtomicOrdering SuccessOrdering, LLVMAtomicOrdering FailureOrdering, LLVMBool SingleThread)
//
// LLVMBool 	LLVMIsAtomicSingleThread (LLVMValueRef AtomicInst)
//
// void 	LLVMSetAtomicSingleThread (LLVMValueRef AtomicInst, LLVMBool SingleThread)
//
// LLVMAtomicOrdering 	LLVMGetCmpXchgSuccessOrdering (LLVMValueRef CmpXchgInst)
//
// void 	LLVMSetCmpXchgSuccessOrdering (LLVMValueRef CmpXchgInst, LLVMAtomicOrdering Ordering)
//
// LLVMAtomicOrdering 	LLVMGetCmpXchgFailureOrdering (LLVMValueRef CmpXchgInst)
//
// void 	LLVMSetCmpXchgFailureOrdering (LLVMValueRef CmpXchgInst, LLVMAtomicOrdering Ordering)
}

// impl Drop for Context {
//     fn drop(&mut self) {
//         unsafe {
//             llvm::core::LLVMContextDispose(self.0);
//         }
//     }
// }
//
// impl Drop for Builder {
//     fn drop(&mut self) {
//         unsafe {
//             llvm::core::LLVMDisposeBuilder(self.0);
//         }
//     }
// }
//
// impl Drop for Module {
//     fn drop(&mut self) {
//         unsafe {
//             llvm::core::LLVMDisposeModule(self.0);
//         }
//     }
// }
