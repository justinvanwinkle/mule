//! Independent Rust callers exercise the same object API used by LLVM programs.

use std::ffi::{CString, c_void};
use std::io::{self, Write};
use std::ptr;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicUsize, Ordering};

use mule_runtime::numeric::{mule_literal_float, mule_literal_int};
use mule_runtime::*;

static SCENARIO: OnceLock<String> = OnceLock::new();
static RECURSION_DEPTH: AtomicUsize = AtomicUsize::new(0);
static ABI_CALLS: AtomicUsize = AtomicUsize::new(0);

unsafe extern "C" {
    #[link_name = "mule.true"]
    static IMMORTAL_TRUE: u8;
    #[link_name = "mule.false"]
    static IMMORTAL_FALSE: u8;
    #[link_name = "mule.none"]
    static IMMORTAL_NONE: u8;
}

unsafe extern "C" fn record_frame(_globals: Value, args: Value) -> Value {
    unsafe {
        assert_eq!(mule_check_args(args, 0), 0);
        ABI_CALLS.fetch_add(1, Ordering::Relaxed);
        mule_none()
    }
}

unsafe fn specialized_abi(globals: Value) {
    unsafe {
        let constants = [
            (mule_bool(1), ptr::addr_of!(IMMORTAL_TRUE) as Value),
            (mule_bool(0), ptr::addr_of!(IMMORTAL_FALSE) as Value),
            (mule_none(), ptr::addr_of!(IMMORTAL_NONE) as Value),
        ];
        for (value, address) in constants {
            assert_eq!(value, address);
            assert_eq!(mule_retain(value), address);
            release(value);
            release(value);
            assert_eq!((*value).references, usize::MAX);
        }
        assert_eq!(mule_retain(small(42)), small(42));
        let owned = text("retained");
        assert_eq!(mule_retain(owned), owned);
        assert_eq!((*owned).references, 2);
        release(owned);
        assert_eq!((*owned).references, 1);
        release(owned);

        let callable = function(globals, record_frame, c"frame");
        let other_globals = namespace();
        let other_scope = function(other_globals, record_frame, c"other_scope");
        let entry = record_frame as *const () as *mut c_void;
        assert_eq!(mule_function_matches(callable, entry, globals), 1);
        assert_eq!(mule_function_matches(other_scope, entry, globals), 0);
        assert_eq!(mule_function_matches(callable, entry, other_globals), 0);
        assert_eq!(
            mule_function_matches(callable, double_value as *const () as *mut c_void, globals),
            0
        );
        for value in [ptr::null_mut(), small(42), mule_none(), mule_bool(1)] {
            assert_eq!(mule_function_matches(value, entry, globals), 0);
        }
        assert!(!has_error());
        error("ValueError", "pending failure");
        assert_eq!(mule_function_matches(callable, entry, globals), 1);
        assert_eq!(error_kind(), "ValueError");
        assert_eq!(error_message(), "pending failure");
        clear_error();

        for _ in 0..999 {
            assert_eq!(mule_enter_frame(), 0);
        }
        let result = mule_call(callable, 0, ptr::null());
        assert!(!result.is_null());
        release(result);
        assert_eq!(ABI_CALLS.load(Ordering::Relaxed), 1);
        assert_eq!(mule_enter_frame(), 0);
        assert!(mule_call(callable, 0, ptr::null()).is_null());
        assert_eq!(error_kind(), "RecursionError");
        assert_eq!(ABI_CALLS.load(Ordering::Relaxed), 1);
        for _ in 0..1000 {
            mule_leave_frame();
        }
        clear_error();
        assert_eq!(mule_enter_frame(), 0);
        mule_leave_frame();
        release(other_scope);
        release(other_globals);
        release(callable);
    }
}

unsafe fn integer(decimal: &str) -> Value {
    let text = CString::new(decimal).expect("probe integer contains no NUL");
    unsafe { mule_literal_int(text.as_ptr()) }
}

unsafe fn text(value: &str) -> Value {
    unsafe { mule_literal_text(value.as_ptr().cast(), value.len() as isize) }
}

unsafe fn show(value: Value) -> bool {
    unsafe {
        if value.is_null() {
            return false;
        }
        let formatted = formatted_value(value, true, false);
        release(value);
        match formatted {
            Some(bytes) => {
                let mut output = io::stdout().lock();
                if output
                    .write_all(&bytes)
                    .and_then(|()| output.write_all(b"\n"))
                    .is_err()
                {
                    error("OSError", "probe output failed");
                    return false;
                }
                true
            }
            None => false,
        }
    }
}

unsafe extern "C" fn add_bias(globals: Value, args: Value) -> Value {
    unsafe {
        if mule_check_args(args, 1) < 0 {
            return ptr::null_mut();
        }
        let value = mule_arg(args, 0);
        let bias = mule_load_global(globals, c"bias".as_ptr());
        if value.is_null() || bias.is_null() {
            release(value);
            release(bias);
            return ptr::null_mut();
        }
        let result = mule_binary(0, value, bias, 0);
        release(value);
        release(bias);
        result
    }
}

unsafe extern "C" fn double_value(_globals: Value, args: Value) -> Value {
    unsafe {
        if mule_check_args(args, 1) < 0 {
            return ptr::null_mut();
        }
        let value = mule_arg(args, 0);
        if value.is_null() {
            return ptr::null_mut();
        }
        let result = mule_binary(0, value, value, 0);
        release(value);
        result
    }
}

unsafe extern "C" fn recurse(globals: Value, args: Value) -> Value {
    unsafe {
        if mule_check_args(args, 0) < 0 {
            return ptr::null_mut();
        }
        if RECURSION_DEPTH.fetch_add(1, Ordering::Relaxed) >= 99_999 {
            error("RuntimeError", "recursion guard missing");
            return ptr::null_mut();
        }
        let function = mule_load_global(globals, c"recurse".as_ptr());
        if function.is_null() {
            return ptr::null_mut();
        }
        let result = mule_call(function, 0, ptr::null());
        release(function);
        result
    }
}

unsafe fn function(globals: Value, entry: Native, name: &std::ffi::CStr) -> Value {
    unsafe { mule_function(globals, entry as *const () as *mut c_void, name.as_ptr()) }
}

unsafe fn function_name(value: Value) -> Value {
    unsafe {
        match &(*value).data {
            Data::Function { name, .. } => text(name),
            _ => {
                error("RuntimeError", "definition lost its function type");
                ptr::null_mut()
            }
        }
    }
}

unsafe extern "C" fn module_entry(globals: Value, args: Value) -> Value {
    unsafe {
        if mule_check_args(args, 0) < 0 {
            return ptr::null_mut();
        }
        match SCENARIO.get().expect("scenario initialized").as_str() {
            "specialized_abi" => {
                specialized_abi(globals);
                println!("abi=ok");
            }
            "literals" => {
                for value in [
                    integer("-123456789012345678901234567890"),
                    mule_literal_float(1.25),
                    text("snow ☃\0end"),
                    mule_literal_bytes(b"a\0b".as_ptr().cast(), 3),
                    mule_none(),
                    mule_bool(0),
                    mule_bool(1),
                ] {
                    if !show(value) {
                        return ptr::null_mut();
                    }
                }
            }
            "huge_int" => {
                let decimal = format!("1{}", "0".repeat(6000));
                let actual = integer(&decimal);
                let ten = integer("10");
                let exponent = integer("6000");
                let expected = mule_binary(7, ten, exponent, 0);
                release(ten);
                release(exponent);
                if actual.is_null() || expected.is_null() {
                    release(actual);
                    release(expected);
                    return ptr::null_mut();
                }
                let equal = mule_compare(0, actual, expected);
                release(actual);
                release(expected);
                if !show(equal) {
                    return ptr::null_mut();
                }
                let negative = integer(&format!("-{decimal}"));
                let zero = integer("0");
                let less = mule_compare(2, negative, zero);
                release(negative);
                release(zero);
                if !show(less) {
                    return ptr::null_mut();
                }
            }
            "recursion" => {
                let callable = function(globals, recurse, c"recurse");
                if callable.is_null() {
                    return ptr::null_mut();
                }
                if mule_store_global(globals, c"recurse".as_ptr(), callable) < 0 {
                    release(callable);
                    return ptr::null_mut();
                }
                let result = mule_call(callable, 0, ptr::null());
                release(callable);
                return result;
            }
            "binary" => {
                let left = integer("7");
                let right = integer("2");
                for op in (0..13).filter(|op| *op != 6) {
                    if !show(mule_binary(op, left, right, 0)) {
                        release(left);
                        release(right);
                        return ptr::null_mut();
                    }
                }
                release(left);
                release(right);
            }
            "unary" => {
                let value = integer("7");
                for op in 0..4 {
                    if !show(mule_unary(op, value)) {
                        release(value);
                        return ptr::null_mut();
                    }
                }
                release(value);
            }
            "compare" => {
                let left = integer("1000000000000000000000");
                let right = integer("1000000000000000000000");
                for op in 0..8 {
                    if !show(mule_compare(op, left, right)) {
                        release(left);
                        release(right);
                        return ptr::null_mut();
                    }
                }
                let items = new_value(Data::Args(vec![retain(left)]));
                let success =
                    show(mule_compare(8, left, items)) && show(mule_compare(9, left, items));
                release(items);
                release(left);
                release(right);
                if !success {
                    return ptr::null_mut();
                }
            }
            "inplace" => {
                let left = mule_list();
                let right = mule_list();
                let value = integer("5");
                if left.is_null()
                    || right.is_null()
                    || value.is_null()
                    || mule_list_append(right, value) < 0
                {
                    release(left);
                    release(right);
                    release(value);
                    return ptr::null_mut();
                }
                let result = mule_binary(0, left, right, 1);
                if result.is_null() {
                    release(left);
                    release(right);
                    release(value);
                    return ptr::null_mut();
                }
                println!("same={}", i32::from(result == left));
                let success = show(result);
                release(left);
                release(right);
                release(value);
                if !success {
                    return ptr::null_mut();
                }
            }
            "globals" => {
                let value = integer("42");
                let status = mule_store_global(globals, c"answer".as_ptr(), value);
                release(value);
                if status < 0 || !show(mule_load_global(globals, c"answer".as_ptr())) {
                    return ptr::null_mut();
                }
                let builtin = mule_load_global(globals, c"print".as_ptr());
                if builtin.is_null() {
                    return ptr::null_mut();
                }
                let callable =
                    !is_fixnum(builtin) && matches!(&(*builtin).data, Data::Function { .. });
                println!("callable={}", i32::from(callable));
                release(builtin);
                let none = mule_none();
                let status = mule_store_global(globals, c"print".as_ptr(), none);
                release(none);
                if status < 0
                    || !show(mule_load_global(globals, c"print".as_ptr()))
                    || !show(mule_load_global(globals, c"__name__".as_ptr()))
                {
                    return ptr::null_mut();
                }
            }
            "function_metadata" => {
                let bias = integer("40");
                let status = mule_store_global(globals, c"bias".as_ptr(), bias);
                release(bias);
                if status < 0 {
                    return ptr::null_mut();
                }
                let first = function(globals, add_bias, c"first");
                if first.is_null() {
                    return ptr::null_mut();
                }
                let mut definitions = 0;
                for _ in 0..10_000 {
                    let replacement = function(globals, add_bias, c"first");
                    if replacement.is_null() {
                        release(first);
                        return ptr::null_mut();
                    }
                    if replacement == first {
                        error("RuntimeError", "definition reused callable identity");
                        release(replacement);
                        release(first);
                        return ptr::null_mut();
                    }
                    let correct = matches!(&(*replacement).data, Data::Function { entry, name, .. }
                        if name == "first" && ptr::fn_addr_eq(*entry, add_bias as Native));
                    if !correct {
                        error("RuntimeError", "replacement lost function metadata");
                        release(replacement);
                        release(first);
                        return ptr::null_mut();
                    }
                    release(replacement);
                    definitions += 1;
                }
                let second = function(globals, add_bias, c"second");
                let other = function(globals, double_value, c"first");
                if second.is_null() || other.is_null() {
                    release(first);
                    release(second);
                    release(other);
                    return ptr::null_mut();
                }
                println!("definitions={definitions}");
                let value = integer("2");
                let values = [value];
                let success = show(function_name(first))
                    && show(function_name(second))
                    && show(mule_call(first, 1, values.as_ptr()))
                    && show(mule_call(second, 1, values.as_ptr()))
                    && show(mule_call(other, 1, values.as_ptr()));
                release(first);
                release(second);
                release(other);
                release(value);
                if !success {
                    return ptr::null_mut();
                }
            }
            "function" | "arity" => {
                let bias = integer("40");
                let status = mule_store_global(globals, c"bias".as_ptr(), bias);
                release(bias);
                if status < 0 {
                    return ptr::null_mut();
                }
                let callable = function(globals, add_bias, c"add_bias");
                if callable.is_null() {
                    return ptr::null_mut();
                }
                if SCENARIO.get().unwrap() == "arity" {
                    let result = mule_call(callable, 0, ptr::null());
                    release(callable);
                    return result;
                }
                let value = integer("2");
                let values = [value];
                let result = mule_call(callable, 1, values.as_ptr());
                release(value);
                release(callable);
                if !show(result) {
                    return ptr::null_mut();
                }
            }
            "builtin_call" => {
                let callable = mule_load_global(globals, c"print".as_ptr());
                let value = text("native callback");
                if callable.is_null() || value.is_null() {
                    release(callable);
                    release(value);
                    return ptr::null_mut();
                }
                let values = [value];
                let result = mule_call(callable, 1, values.as_ptr());
                release(callable);
                release(value);
                if result.is_null() {
                    return ptr::null_mut();
                }
                release(result);
            }
            "truth" => {
                let zero = integer("0");
                let one = integer("1");
                println!("{} {}", mule_truth(zero), mule_truth(one));
                release(zero);
                release(one);
            }
            "zero_division" => {
                let left = integer("1");
                let right = integer("0");
                let result = mule_binary(3, left, right, 0);
                release(left);
                release(right);
                return result;
            }
            "name_error" => return mule_load_global(globals, c"absent_name".as_ptr()),
            "local_error" => return mule_load_local(ptr::null_mut(), c"absent_local".as_ptr()),
            "not_callable" => {
                let value = integer("1");
                let result = mule_call(value, 0, ptr::null());
                release(value);
                return result;
            }
            "release_error" => {
                let value = mule_namespace();
                let owned = text("owned child");
                let stored = bind(value, "child", owned);
                release(owned);
                if value.is_null() || !stored {
                    release(value);
                    return ptr::null_mut();
                }
                error("ValueError", "original failure");
                release(value);
                error("RuntimeError", "cleanup failure");
                return ptr::null_mut();
            }
            "null_without_error" => return ptr::null_mut(),
            _ => {
                error("RuntimeError", "unknown probe");
                return ptr::null_mut();
            }
        }
        mule_none()
    }
}

fn main() {
    let Some(scenario) = std::env::args().nth(1) else {
        std::process::exit(99);
    };
    SCENARIO.set(scenario).expect("scenario set only once");
    let status = unsafe { mule_run(module_entry as *const () as *mut c_void) };
    std::process::exit(status);
}
