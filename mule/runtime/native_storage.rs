//! Physical raw-word allocation and boxing for compiler-selected native layouts.
//! Constructors and field semantics are compiled protocols, not Rust algorithms.

use crate::classes::NativeLayout;
use crate::gmp::BigInt;
use crate::numeric;
use crate::object::{self, Data, Value};
use std::ffi::c_void;

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
            Data::BigInt(integer) => integer.to_i64(),
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
            Data::Float(float) => Some(*float),
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
    unsafe { object::new_value(Data::Float(value)) }
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
