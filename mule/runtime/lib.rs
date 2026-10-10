//! Mule's independent runtime for LLVM-generated Python functions.
//!
//! Tagged integers and owned Rust heap objects use Mule's internal ABI.
//! GMP is the only third-party runtime library; no CPython runtime is linked.

#![deny(unsafe_op_in_unsafe_fn)]

#[cfg(not(all(target_os = "linux", target_pointer_width = "64")))]
compile_error!("Mule's initial runtime target is 64-bit Linux");

pub mod builtins;
pub mod gmp;
pub mod numeric;
pub mod object;

pub use object::*;
