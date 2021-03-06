#![allow(unused_macros)]

use llvm_sys as llvm;

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Once;

use crate::macros::*;

pub mod wrappers;
pub use self::wrappers::*;

static INIT_FAILED: AtomicBool = AtomicBool::new(false);
static INIT: Once = Once::new();

// Intialize the LLVM library.
// It's very important that this be called before any other LLVM functions.
// All the wrapper methods call here.
pub fn init() {
    INIT.call_once(|| {
        unsafe_llvm!({
            if llvm::core::LLVMStartMultithreaded() != 1 {
                INIT_FAILED.store(true, Ordering::SeqCst);
                panic!("Couldn't enable multi-threaded x86 LLVM with MCJIT");
            }

            llvm::target::LLVMInitializeX86TargetInfo();
            llvm::target::LLVMInitializeX86Target();
            llvm::target::LLVMInitializeX86TargetMC();
            llvm::target::LLVMInitializeX86AsmPrinter();
            llvm::target::LLVMInitializeX86AsmParser();

            llvm::execution_engine::LLVMLinkInMCJIT();
        });

        println!("{:?}: LLVM initialized!", std::thread::current().name());
    });

    if INIT_FAILED.load(Ordering::SeqCst) {
        panic!("Couldn't enable multi-threaded x86 LLVM with MCJIT");
    }

    assert_eq!(unsafe { llvm::core::LLVMIsMultithreaded() }, 1);

    println!("{:?}: LLVM (re)initialized!", std::thread::current().name());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn smoke_test() {
        let context = Context::new();

        // Set up a context, module and builder in that context.
        let module = Module::new("nop");
        let builder = context.new_builder();

        // Get the type signature for void nop(void);
        // Then create it in our module.
        let void = context.void_type();
        let function_type = Type::function(void, &[], false);
        let function = module.add_function("nop", function_type);

        // Create a basic block in the function and set our builder to generate
        // code in it.
        let bb = context.append_bb(function, "entry");
        builder.position_at_end(bb);

        // Emit a `ret void` into the function
        builder.ret_void();

        // Dump the module as IR to stdout.
        // module.dump();
    }

    // #[test]
    // pub fn test_bc() {
    //     let context = Context::new();
    //     let module = Module::new("main");
    //     let builder = context.new_builder();
    //     builder.write_bitcode_to_file("main.bc");
    // }
}
