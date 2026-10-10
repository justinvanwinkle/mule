//! Mule's independent runtime for LLVM-generated Python functions.
//!
//! Tagged integers and owned Rust heap objects use Mule's internal ABI.
//! GMP is the only third-party runtime library; no CPython runtime is linked.

#![deny(unsafe_op_in_unsafe_fn)]

#[cfg(not(all(target_os = "linux", target_pointer_width = "64")))]
compile_error!("Mule's initial runtime target is 64-bit Linux");

pub mod annotations;
pub mod builtins;
pub mod classes;
pub mod gmp;
pub mod native_storage;
pub mod numeric;
pub mod object;
pub mod sequences;
pub use sequences::*;

pub use annotations::*;
pub use classes::*;
pub use native_storage::*;
pub use object::*;
