//! Borrowed native startup arguments; compiled Python owns the language-level list.

use crate::numeric;
use crate::object::{self, Data, Value};
use std::cell::Cell;
use std::ffi::{CStr, c_char};
use std::ptr::null_mut;

#[derive(Clone, Copy)]
struct Arguments {
    count: usize,
    values: *const *const c_char,
}

// The current runtime executes compiled Python on the native main thread only.
// Native main's argument vector remains valid for the process lifetime.
thread_local! {
    static ARGUMENTS: Cell<Option<Arguments>> = const { Cell::new(None) };
}

fn fail(kind: &str, message: &str) -> Value {
    object::error(kind, message);
    null_mut()
}

fn arguments() -> Option<Arguments> {
    let arguments = ARGUMENTS.get();
    if arguments.is_none() {
        object::error(
            "SystemError",
            "native process arguments are not initialized",
        );
    }
    arguments
}

/// Publish native main's arguments before any compiled language initialization.
///
/// # Safety
/// A nonempty vector must contain `count` valid pointers to NUL-terminated bytes.
/// The vector and its strings must remain live while argument access is possible.
/// Call this and the argument access primitives on the native main thread.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_set_process_arguments(count: i32, values: *const *const c_char) {
    if count < 0 || (count > 0 && values.is_null()) {
        ARGUMENTS.set(None);
        object::error("SystemError", "invalid native process argument vector");
        return;
    }
    ARGUMENTS.set(Some(Arguments {
        count: count as usize,
        values,
    }));
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_process_argument_count() -> Value {
    let Some(arguments) = arguments() else {
        return null_mut();
    };
    unsafe { numeric::integer_from_usize(arguments.count) }
}

/// Copy one borrowed native argument into an owned Mule text value.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn mule_process_argument(index: Value) -> Value {
    let Some(arguments) = arguments() else {
        return null_mut();
    };
    let Some(index) = (unsafe { numeric::integer_export(index) }) else {
        return null_mut();
    };
    let Some(index) = index.to_i64() else {
        return fail("IndexError", "native process argument index out of range");
    };
    if index < 0 || index as usize >= arguments.count {
        return fail("IndexError", "native process argument index out of range");
    }
    let pointer = unsafe { *arguments.values.add(index as usize) };
    if pointer.is_null() {
        return fail("SystemError", "native process argument is a null pointer");
    }
    let bytes = unsafe { CStr::from_ptr(pointer) }.to_bytes();
    if std::str::from_utf8(bytes).is_err() {
        return fail(
            "UnicodeDecodeError",
            "native process argument is not valid UTF-8",
        );
    }
    unsafe { object::new_value(Data::Text(bytes.to_vec())) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn argument_access_distinguishes_uninitialized_and_empty_vectors() {
        ARGUMENTS.set(None);
        object::clear_error();
        assert!(unsafe { mule_process_argument_count() }.is_null());
        assert_eq!(object::error_kind(), "SystemError");
        object::clear_error();
        unsafe { mule_set_process_arguments(0, std::ptr::null()) };
        let count = unsafe { mule_process_argument_count() };
        assert_eq!(unsafe { object::fixnum(count) }, 0);
        assert!(unsafe { mule_process_argument(object::small(0)) }.is_null());
        assert_eq!(object::error_kind(), "IndexError");
        object::clear_error();
        ARGUMENTS.set(None);
    }

    #[test]
    fn argument_access_checks_indices_and_strict_utf8() {
        let vector = [c"valid".as_ptr(), b"\xed\xa0\x80\0".as_ptr().cast()];
        object::clear_error();
        unsafe { mule_set_process_arguments(vector.len() as i32, vector.as_ptr()) };
        let value = unsafe { mule_process_argument(object::small(0)) };
        assert!(!value.is_null());
        assert!(matches!(unsafe { &(*value).data }, Data::Text(bytes) if bytes == b"valid"));
        unsafe { object::release(value) };
        for index in [-1, 2] {
            assert!(unsafe { mule_process_argument(object::small(index)) }.is_null());
            assert_eq!(object::error_kind(), "IndexError");
            object::clear_error();
        }
        let text = unsafe { object::new_value(Data::Text(b"0".to_vec())) };
        assert!(unsafe { mule_process_argument(text) }.is_null());
        assert_eq!(object::error_kind(), "TypeError");
        unsafe { object::release(text) };
        object::clear_error();
        assert!(unsafe { mule_process_argument(object::small(1)) }.is_null());
        assert_eq!(object::error_kind(), "UnicodeDecodeError");
        object::clear_error();
        ARGUMENTS.set(None);
    }
}
