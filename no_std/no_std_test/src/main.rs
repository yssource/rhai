//! This is a `no-std` application for the that evaluates
//! a simple expression and uses the result as the return value.

#![no_std]
#![feature(alloc_error_handler, start, core_intrinsics, lang_items)]

extern crate alloc;
extern crate wee_alloc;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

use rhai::{Engine, INT};

#[start]
fn main(_argc: isize, _argv: *const *const u8) -> isize {
    let engine = Engine::new();

    // Evaluate a simple expression: 40 + 2
    engine.eval::<INT>("40 + 2").unwrap() as isize
}

#[alloc_error_handler]
fn foo(_: core::alloc::Layout) -> ! {
    core::intrinsics::abort();
}

#[panic_handler]
#[lang = "panic_impl"]
extern "C" fn rust_begin_panic(_: &core::panic::PanicInfo) -> ! {
    core::intrinsics::abort();
}

#[lang = "eh_personality"]
extern "C" fn eh_personality() {}

#[no_mangle]
extern "C" fn rust_eh_register_frames() {}

#[no_mangle]
extern "C" fn rust_eh_unregister_frames() {}
