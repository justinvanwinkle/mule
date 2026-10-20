//! Physical raw-word allocation and boxing for compiler-selected native layouts.
//! Constructors and field semantics are compiled protocols, not Rust algorithms.

use crate::classes::NativeLayout;
use crate::gmp::BigInt;
use crate::numeric;
use crate::object::{self, Data, Value};
use std::ffi::c_void;

/// Acquire uninitialized object storage from the runtime's physical allocator.
/// The caller must initialize every active header and payload field before
/// publishing the pointer or using any operation that reads an Object.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_allocate_object_storage() -> Value {
    unsafe { object::allocate_object_storage() }
}

/// Release a terminal raw allocation after all required object cleanup.
/// This does not decrement references, drop payloads, or clear pending errors.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_deallocate_object_storage(value: Value) {
    unsafe { object::deallocate_object_storage(value) }
}

/// Acquire uninitialized limb bytes using GMP's configured allocation callback.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_allocate_integer_limbs(bytes: usize) -> *mut u64 {
    unsafe { crate::gmp::allocate_limbs(bytes).cast() }
}

/// Release terminal limb storage through GMP's matching allocation callback.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_deallocate_integer_limbs(limbs: *mut u64, bytes: usize) {
    unsafe { crate::gmp::deallocate_limbs(limbs.cast(), bytes) }
}

/// Drop an initialized BigInt payload without inspecting its surrounding Object.
/// Header ownership, class/attribute references, GC and weakrefs belong to callers.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_clear_integer_storage(storage: *mut c_void) {
    unsafe { std::ptr::drop_in_place(storage.cast::<BigInt>()) }
}

unsafe fn descriptor<'a>(class: Value) -> Option<&'a NativeLayout> {
    if class.is_null() || object::is_fixnum(class) {
        return None;
    }
    match unsafe { &(*class).data } {
        Data::Class(layout) => layout.native.as_ref(),
        _ => None,
    }
}

/// Guards physical layout identity without dereferencing the schema or globals.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_native_class_matches(
    class: Value,
    schema: *const c_void,
    globals: Value,
) -> i32 {
    i32::from(
        unsafe { descriptor(class) }
            .is_some_and(|layout| layout.schema == schema && layout.globals == globals),
    )
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_native_instance_matches(
    instance: Value,
    schema: *const c_void,
    globals: Value,
) -> i32 {
    if instance.is_null() || object::is_fixnum(instance) {
        return 0;
    }
    match unsafe { &(*instance).data } {
        Data::NativeInstance { ty, .. } => unsafe {
            mule_native_class_matches(*ty, schema, globals)
        },
        _ => 0,
    }
}

/// Allocate zeroed raw words and an owned descriptor reference. The compiled
/// constructor is responsible for completing initialization before publication.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_native_new(class: Value) -> Value {
    if object::has_error() {
        return std::ptr::null_mut();
    }
    if unsafe { descriptor(class) }.is_none() {
        mule_native_type_error();
        return std::ptr::null_mut();
    }
    let Data::Class(layout) = (unsafe { &(*class).data }) else {
        unreachable!();
    };
    let mut payload = Vec::new();
    if payload.try_reserve_exact(layout.fields.len()).is_err() {
        object::error("MemoryError", "native payload allocation failed");
        return std::ptr::null_mut();
    }
    payload.resize(layout.fields.len(), 0);
    unsafe {
        object::new_value(Data::NativeInstance {
            ty: object::retain(class),
            payload: payload.into_boxed_slice(),
        })
    }
}

/// Borrow a payload pointer. Its lifetime is bounded by the owned instance.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_native_payload(instance: Value) -> *mut i64 {
    if !instance.is_null() && !object::is_fixnum(instance) {
        if let Data::NativeInstance { payload, .. } = unsafe { &mut (*instance).data } {
            return payload.as_mut_ptr();
        }
    }
    mule_native_type_error();
    std::ptr::null_mut()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_native_instance_payload(
    instance: Value,
    schema: *const c_void,
    globals: Value,
) -> *mut i64 {
    if unsafe { mule_native_instance_matches(instance, schema, globals) } == 0 {
        mule_native_type_error();
        std::ptr::null_mut()
    } else {
        unsafe { mule_native_payload(instance) }
    }
}

/// Extract the signed 64-bit physical integer representation into a borrowed
/// output word. No conversion of floats or booleans is performed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_unbox_i64(value: Value, output: *mut i64) -> i32 {
    if object::has_error() {
        return -1;
    }
    if output.is_null() {
        object::error("SystemError", "NULL integer output");
        return -1;
    }
    let integer = if object::is_fixnum(value) {
        Some(unsafe { object::fixnum(value) })
    } else if value.is_null() {
        None
    } else {
        match unsafe { &(*value).data } {
            Data::BigInt(integer, ..) => integer.to_i64(),
            _ => None,
        }
    };
    if let Some(integer) = integer {
        unsafe { *output = integer };
        0
    } else {
        object::error("TypeError", "expected mule.int64");
        -1
    }
}

/// Publish a raw integer as an owned generic value; immediate values need no box.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_box_i64(value: i64) -> Value {
    if object::has_error() {
        return std::ptr::null_mut();
    }
    if (numeric::MIN..=numeric::MAX).contains(&value) {
        object::small(value)
    } else {
        unsafe { numeric::integer_import_owned(BigInt::from_i64(value)) }
    }
}

/// Extract the binary64 physical representation of a float into a borrowed
/// output word. Integers are not converted: the compiler promotes them itself.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_unbox_f64(value: Value, output: *mut f64) -> i32 {
    if object::has_error() {
        return -1;
    }
    if output.is_null() {
        object::error("SystemError", "NULL float output");
        return -1;
    }
    let float = if value.is_null() || object::is_fixnum(value) {
        None
    } else {
        match unsafe { &(*value).data } {
            Data::Float(float, ..) => Some(*float),
            _ => None,
        }
    };
    if let Some(float) = float {
        unsafe { *output = float };
        0
    } else {
        object::error("TypeError", "expected mule.float64");
        -1
    }
}

/// Publish a raw binary64 float as an owned generic value.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_box_f64(value: f64) -> Value {
    if object::has_error() {
        return std::ptr::null_mut();
    }
    unsafe { object::new_value(Data::Float(value, std::ptr::null_mut(), std::ptr::null_mut())) }
}

/// Publish a binary64 result, reusing an exclusive operand or the two references
/// proven by the compiler to be an owned local and its arithmetic operand.
/// The operand is consumed on every path; the local argument is borrowed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_box_f64_move(value: f64, operand: Value, local: Value) -> Value {
    if object::has_error() {
        unsafe { object::release(operand) };
        return std::ptr::null_mut();
    }
    unsafe {
        if !operand.is_null() && !object::is_fixnum(operand) {
            let references = (*operand).reference_count();
            let exclusive = references == 1 || references == 2 && operand == local;
            if let Data::Float(payload, class, attributes) = &mut (*operand).data {
                if exclusive && class.is_null() && attributes.is_null() {
                    *payload = value;
                    return operand;
                }
            }
        }
        let result = mule_box_f64(value);
        object::release(operand);
        result
    }
}

#[cfg(test)]
mod float_box_tests {
    use super::*;

    unsafe fn initialize_float_storage(storage: Value, payload: f64) {
        unsafe {
            storage.write(object::Object {
                references: std::sync::atomic::AtomicUsize::new(1),
                tracked: false,
                gc_slot: 0,
                data: Data::Float(payload, std::ptr::null_mut(), std::ptr::null_mut()),
            });
        }
    }

    #[test]
    fn initialized_raw_storage_supports_normal_unboxing_and_release() {
        unsafe {
            let storage = mule_allocate_object_storage();
            assert!(!storage.is_null());
            assert_eq!(storage as usize % std::mem::align_of::<object::Object>(), 0);
            initialize_float_storage(storage, 3.25);
            let mut output = 0.0;
            assert_eq!(mule_unbox_f64(storage, &mut output), 0);
            assert_eq!(output, 3.25);
            object::release(storage);
        }
    }

    #[test]
    fn terminal_leaf_storage_returns_to_the_existing_pool() {
        unsafe {
            assert!(!object::has_error());
            object::flush_object_pool();
            let first = mule_allocate_object_storage();
            initialize_float_storage(first, 1.5);
            mule_deallocate_object_storage(first);
            let reused = mule_allocate_object_storage();
            assert_eq!(reused, first);
            initialize_float_storage(reused, -0.0);
            let mut output = 0.0;
            assert_eq!(mule_unbox_f64(reused, &mut output), 0);
            assert_eq!(output.to_bits(), (-0.0_f64).to_bits());
            object::release(reused);
        }
    }

    #[test]
    fn raw_storage_operations_preserve_a_pending_error() {
        unsafe {
            assert!(!object::has_error());
            object::error("ValueError", "raw storage pending error");
            let storage = mule_allocate_object_storage();
            assert!(!storage.is_null());
            mule_deallocate_object_storage(storage);
            assert!(object::has_error());
            assert_eq!(object::error_kind(), "ValueError");
            assert_eq!(object::error_message(), "raw storage pending error");
            object::clear_error();
        }
    }

    #[test]
    fn an_exclusive_moved_operand_reuses_its_box() {
        unsafe {
            let operand = mule_box_f64(1.5);
            let result = mule_box_f64_move(2.0, operand, std::ptr::null_mut());
            assert_eq!(result, operand);
            assert_eq!((*result).reference_count(), 1);
            object::release(result);
        }
    }

    #[test]
    fn proven_local_and_operand_reuse_the_box_with_owned_result() {
        unsafe {
            let local = mule_box_f64(1.5);
            let operand = object::retain(local);
            let result = mule_box_f64_move(-0.0, operand, local);
            assert_eq!(result, local);
            assert_eq!((*result).reference_count(), 2);
            let Data::Float(payload, ..) = &(*result).data else { panic!("missing payload"); };
            assert_eq!(payload.to_bits(), (-0.0_f64).to_bits());
            object::release(local);
            object::release(result);
        }
    }

    #[test]
    fn an_extra_alias_preserves_the_old_payload() {
        unsafe {
            let local = mule_box_f64(1.5);
            let operand = object::retain(local);
            let alias = object::retain(local);
            let result = mule_box_f64_move(2.0, operand, local);
            assert_ne!(result, local);
            let Data::Float(payload, ..) = &(*alias).data else { panic!("missing payload"); };
            assert_eq!(*payload, 1.5);
            object::release(alias);
            object::release(local);
            object::release(result);
        }
    }

    #[test]
    fn two_references_without_the_local_proof_do_not_reuse() {
        unsafe {
            let original = mule_box_f64(1.5);
            let operand = object::retain(original);
            let result = mule_box_f64_move(2.0, operand, std::ptr::null_mut());
            assert_ne!(result, original);
            object::release(original);
            object::release(result);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn mule_native_zero_division() -> i32 {
    object::error("ZeroDivisionError", "float division by zero");
    -1
}

#[unsafe(no_mangle)]
pub extern "C" fn mule_native_type_error() -> i32 {
    object::error("TypeError", "expected native class layout");
    -1
}

#[unsafe(no_mangle)]
pub extern "C" fn mule_native_initializer_error() -> i32 {
    object::error("TypeError", "__init__ must return None");
    -1
}

#[unsafe(no_mangle)]
pub extern "C" fn mule_native_overflow() -> i32 {
    object::error("OverflowError", "native int64 arithmetic overflow");
    -1
}
