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

/// Read a global through its linked cell, as compiled code does.
unsafe fn load_global(namespace: Value, name: *const std::ffi::c_char) -> Value {
    unsafe { mule_load_global(linked(namespace, name), name) }
}

/// Bind a global through its linked cell, as compiled code does.
unsafe fn store_global(namespace: Value, name: *const std::ffi::c_char, value: Value) -> i32 {
    unsafe { mule_store_global(linked(namespace, name), name, value) }
}

/// The cell a unit would link for `name` in `namespace`, linked on first
/// use. A unit's cells are static, so these are leaked to outlive it.
unsafe fn linked(namespace: Value, name: *const std::ffi::c_char) -> *mut GlobalCell {
    thread_local! {
        static CELLS: std::cell::RefCell<std::collections::HashMap<(usize, Vec<u8>), usize>> =
            std::cell::RefCell::new(std::collections::HashMap::new());
    }
    let key = (namespace as usize, unsafe { std::ffi::CStr::from_ptr(name) }.to_bytes().to_vec());
    if let Some(cell) = CELLS.with_borrow(|cells| cells.get(&key).copied()) {
        return cell as *mut GlobalCell;
    }
    let cell = Box::leak(Box::new(std::mem::MaybeUninit::<GlobalCell>::uninit())).as_mut_ptr();
    let status = unsafe { mule_link_globals(namespace, 1, &name, &cell, 0) };
    assert_eq!(status, 0, "linking a global cell failed");
    CELLS.with_borrow_mut(|cells| cells.insert(key, cell as usize));
    cell
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
            assert_eq!((*value).reference_count(), usize::MAX);
        }
        assert_eq!(mule_retain(small(42)), small(42));
        let owned = text("retained");
        assert_eq!(mule_retain(owned), owned);
        assert_eq!((*owned).reference_count(), 2);
        release(owned);
        assert_eq!((*owned).reference_count(), 1);
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
        bigint_reuse();
    }
}

unsafe fn bigint_reuse() {
    unsafe {
        let decimal = "1267650600228229401496703205376";
        let check = |value, expected: &str| {
            assert_eq!(
                formatted_value(value, false, false).unwrap(),
                expected.as_bytes()
            );
        };

        // One owned temporary can donate its existing allocation. The returned
        // ownership survives release of the consumed operand.
        let left = integer(decimal);
        let result = mule_binary_reuse(2, left, small(-2), 0, ptr::null_mut());
        assert_eq!(result, left);
        assert_eq!((*left).reference_count(), 2);
        release(left);
        assert_eq!((*result).reference_count(), 1);
        check(result, "-2535301200456458802993406410752");
        release(result);

        // An immediately overwritten local and its operand own exactly two
        // references. Follow the same release/store sequence as LLVM.
        let mut slot = integer(decimal);
        let left = retain(slot);
        let result = mule_binary_reuse(0, left, small(7), 0, &mut slot);
        assert_eq!(result, left);
        assert_eq!((*result).reference_count(), 3);
        release(left);
        release(slot);
        slot = result;
        assert_eq!((*slot).reference_count(), 1);
        check(slot, "1267650600228229401496703205383");
        release(slot);

        // A third owner must keep the original immutable, even for += and a
        // valid overwrite slot. Caller-visible aliases retain their value.
        let mut slot = integer(decimal);
        let alias = retain(slot);
        let left = retain(slot);
        let result = mule_binary_reuse(1, left, small(7), 1, &mut slot);
        assert_ne!(result, left);
        check(alias, decimal);
        check(result, "1267650600228229401496703205369");
        release(left);
        release(slot);
        slot = result;
        release(slot);
        release(alias);

        // Refcount alone cannot prove a local overwrite. An absent or rebound
        // slot forbids the two-owner reuse, and an aliased RHS always forbids it.
        for mode in 0..3 {
            let original = integer(decimal);
            let left = retain(original);
            let mut other_slot = small(99);
            let result = match mode {
                0 => mule_binary_reuse(2, left, small(2), 1, ptr::null_mut()),
                1 => mule_binary_reuse(2, left, small(2), 0, &mut other_slot),
                _ => {
                    let mut slot = original;
                    mule_binary_reuse(2, left, left, 0, &mut slot)
                }
            };
            assert_ne!(result, original);
            check(original, decimal);
            assert_eq!(other_slot, small(99));
            release(result);
            release(left);
            release(original);
        }

        // RHS evaluation can replace the original local. If its former value
        // is now uniquely owned, reusing that detached value is still safe.
        let left = integer(decimal);
        let mut replacement = integer("2535301200456458802993406410752");
        let result = mule_binary_reuse(2, left, small(2), 0, &mut replacement);
        assert_eq!(result, left);
        check(replacement, "2535301200456458802993406410752");
        release(left);
        release(result);
        release(replacement);

        // A smaller result becomes a tag; the old boxed operand is consumed by
        // the normal ownership protocol rather than retained for the result.
        let left = integer("4611686018427387904");
        let result = mule_binary_reuse(1, left, small(1), 0, ptr::null_mut());
        assert_eq!(result, small(4611686018427387903));
        assert_eq!((*left).reference_count(), 1);
        release(left);

        let left = integer(decimal);
        let right = integer("18446744073709551616");
        let result = mule_binary_reuse(2, left, right, 0, ptr::null_mut());
        assert_eq!(result, left);
        check(result, "23384026197294446691258957323460528314494920687616");
        check(right, "18446744073709551616");
        release(left);
        release(result);
        release(right);

        // Mixed Bool inputs and operations that can raise use the ordinary
        // immutable path. Sticky errors prevent all mutation.
        let left = integer(decimal);
        let result = mule_binary_reuse(2, left, mule_bool(1), 0, ptr::null_mut());
        assert_ne!(result, left);
        check(left, decimal);
        release(result);
        assert!(mule_binary_reuse(4, left, small(0), 0, ptr::null_mut()).is_null());
        assert_eq!(error_kind(), "ZeroDivisionError");
        check(left, decimal);
        assert!(mule_binary_reuse(2, left, small(2), 0, ptr::null_mut()).is_null());
        check(left, decimal);
        clear_error();
        release(left);
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
        let bias = load_global(globals, c"bias".as_ptr());
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
        let function = load_global(globals, c"recurse".as_ptr());
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

/// A class statement without keywords: `bases` become the bases tuple.
unsafe fn class_statement(
    name: &std::ffi::CStr,
    bases: &[Value],
    fields: &[*const std::ffi::c_char],
    namespace: Value,
) -> Value {
    unsafe {
        let bases = mule_sequence_tuple(bases.len() as isize, bases.as_ptr());
        let class = mule_class(
            name.as_ptr(),
            bases,
            fields.len() as isize,
            fields.as_ptr(),
            namespace,
            mule_none(),
        );
        release(bases);
        class
    }
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

unsafe extern "C" fn initialize_slot(_: Value, args: Value) -> Value {
    unsafe {
        if mule_check_args(args, 2) < 0 {
            return ptr::null_mut();
        }
        let receiver = argument(args, 0).unwrap();
        let value = argument(args, 1).unwrap();
        if mule_set_attr(receiver, c"value".as_ptr(), value) < 0 {
            return ptr::null_mut();
        }
        mule_none()
    }
}

unsafe extern "C" fn add_slot(_: Value, args: Value) -> Value {
    unsafe {
        if mule_check_args(args, 2) < 0 {
            return ptr::null_mut();
        }
        let receiver = argument(args, 0).unwrap();
        let value = mule_get_attr(receiver, c"value".as_ptr());
        if value.is_null() {
            return value;
        }
        let result = mule_binary(0, value, argument(args, 1).unwrap(), 0);
        release(value);
        result
    }
}

unsafe fn class_ownership(globals: Value) {
    unsafe {
        let init = function(globals, initialize_slot, c"initialize_slot");
        let add = function(globals, add_slot, c"add_slot");
        let fields = [c"value".as_ptr(), c"callback".as_ptr()];
        let namespace = mule_namespace();
        assert_eq!(store_global(namespace, c"__init__".as_ptr(), init), 0);
        assert_eq!(store_global(namespace, c"add".as_ptr(), add), 0);
        let base = class_statement(c"Base", &[], &fields, namespace);
        assert!(!base.is_null());
        release(namespace);
        release(init);
        release(add);
        let extra = [c"extra".as_ptr()];
        let namespace = mule_namespace();
        let derived = class_statement(c"Derived", &[base], &extra, namespace);
        release(namespace);
        let values = [small(42)];
        let instance = mule_call(derived, 1, values.as_ptr());
        assert!(!instance.is_null());
        let mut cache = AttrCache::default();
        assert_eq!(
            mule_get_attr_cached(instance, c"value".as_ptr(), &mut cache),
            small(42)
        );
        let first_id = cache.class_id;
        assert_ne!(first_id, 0);
        assert_eq!(mule_set_attr(instance, c"extra".as_ptr(), small(7)), 0);
        assert_eq!(mule_get_attr(instance, c"extra".as_ptr()), small(7));

        let mut method_cache = AttrCache::default();
        let argument = [small(8)];
        assert_eq!(
            mule_call_method_cached(
                instance,
                c"add".as_ptr(),
                1,
                argument.as_ptr(),
                &mut method_cache
            ),
            small(50)
        );
        let bound = mule_get_attr(instance, c"add".as_ptr());
        assert_eq!((*instance).reference_count(), 2);
        assert_eq!(mule_call(bound, 1, argument.as_ptr()), small(50));

        let old_callback = function(globals, double_value, c"old_callback");
        assert_eq!(
            mule_set_attr(instance, c"callback".as_ptr(), old_callback),
            0
        );
        release(old_callback);
        let mut receiver = ptr::null_mut();
        let callback = mule_resolve_method(
            instance,
            c"callback".as_ptr(),
            ptr::null_mut(),
            &mut receiver,
        );
        assert!(receiver.is_null());
        let replacement = function(globals, record_frame, c"replacement");
        assert_eq!(
            mule_set_attr(instance, c"callback".as_ptr(), replacement),
            0
        );
        release(replacement);
        assert_eq!(
            mule_call_resolved(callback, receiver, 1, argument.as_ptr()),
            small(16)
        );
        release(callback);

        // Each slot owns one reference; reads own another. Replacing a slot
        // releases its previous value without invalidating a saved read.
        let owned = text("slot ownership");
        assert_eq!(
            mule_set_attr_cached(instance, c"value".as_ptr(), owned, &mut cache),
            0
        );
        assert_eq!((*owned).reference_count(), 2);
        let saved = mule_get_attr_cached(instance, c"value".as_ptr(), &mut cache);
        assert_eq!(saved, owned);
        assert_eq!((*owned).reference_count(), 3);
        assert_eq!(
            mule_set_attr_cached(instance, c"value".as_ptr(), small(42), &mut cache),
            0
        );
        assert_eq!((*owned).reference_count(), 2);
        release(saved);
        release(owned);

        // The bound method keeps its receiver and inherited class alive.
        release(instance);
        release(derived);
        release(base);
        assert_eq!(mule_call(bound, 1, argument.as_ptr()), small(50));
        release(bound);

        // An expired class cannot validate a reused site's cached slot index.
        let new_fields = [c"unused".as_ptr(), c"value".as_ptr()];
        let namespace = mule_namespace();
        let later = class_statement(c"Later", &[], &new_fields, namespace);
        release(namespace);
        let later_instance = mule_call(later, 0, ptr::null());
        assert_eq!(
            mule_set_attr(later_instance, c"value".as_ptr(), small(99)),
            0
        );
        assert_eq!(
            mule_get_attr_cached(later_instance, c"value".as_ptr(), &mut cache),
            small(99)
        );
        assert_ne!(cache.class_id, first_id);
        assert!(mule_get_attr(later_instance, c"unused".as_ptr()).is_null());
        assert_eq!(error_kind(), "AttributeError");
        clear_error();
        assert_eq!(
            mule_set_attr(later_instance, c"unknown".as_ptr(), small(1)),
            -1
        );
        assert_eq!(error_kind(), "AttributeError");
        clear_error();
        // Class attributes are assignable; the instance's own field still wins.
        assert_eq!(mule_set_attr(later, c"value".as_ptr(), small(1)), 0);
        assert_eq!(mule_get_attr(later, c"value".as_ptr()), small(1));
        assert_eq!(
            mule_get_attr_cached(later_instance, c"value".as_ptr(), &mut cache),
            small(99)
        );
        release(later_instance);
        release(later);
        assert!(!has_error());
    }
}

unsafe fn dynamic_tagged_attributes() {
    unsafe {
        let attributes = namespace();
        assert!(bind(attributes, "number", small(42)));
        assert!(bind(attributes, "__bool__", small(42)));
        let class = class_statement(c"Dynamic", &[], &[], attributes);
        release(attributes);
        assert_eq!(mule_get_attr(class, c"number".as_ptr()), small(42));
        let instance = mule_call(class, 0, ptr::null());
        assert_eq!(mule_get_attr(instance, c"number".as_ptr()), small(42));
        assert_eq!(mule_truth(instance), -1);
        assert_eq!(error_kind(), "TypeError");
        clear_error();
        release(instance);
        release(class);
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
            "class_ownership" => {
                class_ownership(globals);
                println!("classes=ok");
            }
            "dynamic_tagged_attributes" => {
                dynamic_tagged_attributes();
                println!("dynamic=ok");
            }
            "tuple_slots" => {
                let owned = text("owned child");
                let values = [owned, mule_none()];
                let tuple = mule_sequence_tuple(2, values.as_ptr());
                release(owned);
                assert_eq!(mule_tuple_shape_matches(tuple, 2), 1);
                assert_eq!(mule_tuple_shape_matches(tuple, 1), 0);
                assert_eq!(mule_tuple_shape_matches(small(2), 2), 0);
                let list = mule_sequence_list(2, values.as_ptr());
                assert_eq!(mule_tuple_shape_matches(list, 2), 0);
                release(list);
                let child = mule_tuple_slot(tuple, 0);
                let none = mule_tuple_slot(tuple, 1);
                release(tuple);
                assert_eq!(none, mule_none());
                release(none);
                if !show(child) {
                    return ptr::null_mut();
                }
            }
            "owned_tuple" => {
                let child = text("transferred child");
                let values = [retain(child), retain(child)];
                let tuple = mule_sequence_tuple_owned(2, values.as_ptr());
                assert!(!tuple.is_null());
                assert_eq!((*child).reference_count(), 3);
                let extracted = mule_tuple_slot(tuple, 1);
                release(tuple);
                release(child);
                assert_eq!((*extracted).reference_count(), 1);
                if !show(extracted) {
                    return ptr::null_mut();
                }
                let empty = mule_sequence_tuple_owned(0, ptr::null());
                assert_eq!(mule_tuple_shape_matches(empty, 0), 1);
                release(empty);

                let held = text("pending failure");
                let transferred = [retain(held)];
                error("ValueError", "original failure");
                let pending_tuple = mule_sequence_tuple_owned(1, transferred.as_ptr());
                assert!(!pending_tuple.is_null());
                assert_eq!(error_kind(), "ValueError");
                release(pending_tuple);
                assert_eq!((*held).reference_count(), 1);
                clear_error();
                release(held);
                assert!(mule_sequence_tuple_owned(-1, ptr::null()).is_null());
                assert_eq!(error_kind(), "SystemError");
                clear_error();
                assert!(mule_sequence_tuple_owned(1, ptr::null()).is_null());
                assert_eq!(error_kind(), "SystemError");
                clear_error();
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
                if store_global(globals, c"recurse".as_ptr(), callable) < 0 {
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
                let items = new_value(Data::Args(vec![retain(left)], ptr::null_mut(), ptr::null_mut()));
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
                let status = store_global(globals, c"answer".as_ptr(), value);
                release(value);
                if status < 0 || !show(load_global(globals, c"answer".as_ptr())) {
                    return ptr::null_mut();
                }
                let builtin = load_global(globals, c"len".as_ptr());
                if builtin.is_null() {
                    return ptr::null_mut();
                }
                let callable =
                    !is_fixnum(builtin) && matches!(&(*builtin).data, Data::Function { .. });
                println!("callable={}", i32::from(callable));
                release(builtin);
                let none = mule_none();
                let status = store_global(globals, c"len".as_ptr(), none);
                release(none);
                if status < 0
                    || !show(load_global(globals, c"len".as_ptr()))
                    || !show(load_global(globals, c"__name__".as_ptr()))
                {
                    return ptr::null_mut();
                }
            }
            "function_metadata" => {
                let bias = integer("40");
                let status = store_global(globals, c"bias".as_ptr(), bias);
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
                let status = store_global(globals, c"bias".as_ptr(), bias);
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
                let callable = load_global(globals, c"len".as_ptr());
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
                if !show(result) {
                    return ptr::null_mut();
                }
            }
            "native_safepoint_handshake" => {
                use mule_runtime::threads;
                use std::sync::{atomic::AtomicBool, mpsc};
                use std::time::Duration;
                let finish = AtomicBool::new(false);
                let (ready_send, ready) = mpsc::channel();
                let (done_send, done) = mpsc::channel();
                std::thread::scope(|scope| {
                    let finish = &finish;
                    scope.spawn(move || {
                        let _registration = threads::Registration::enter();
                        ready_send.send(()).unwrap();
                        loop {
                            if threads::SAFEPOINT_REQUESTED.load(Ordering::Acquire) {
                                threads::mule_safepoint();
                            }
                            if finish.load(Ordering::Acquire) { break; }
                        }
                        done_send.send(()).unwrap();
                    });
                    ready.recv().unwrap();
                    threads::with_world_stopped(|| {
                        assert!(threads::SAFEPOINT_REQUESTED.load(Ordering::Acquire));
                        finish.store(true, Ordering::Release);
                        assert!(matches!(done.try_recv(), Err(mpsc::TryRecvError::Empty)));
                    });
                    assert!(!threads::SAFEPOINT_REQUESTED.load(Ordering::Acquire));
                    done.recv_timeout(Duration::from_secs(10)).unwrap();
                });
                println!("native safepoint=ok");
            }
            "shared_heap_registry" => {
                use mule_runtime::{gc, object, threads};
                use std::sync::mpsc;
                let (ready_send, ready) = mpsc::channel();
                let (finish_send, finish) = mpsc::channel();
                std::thread::scope(|scope| {
                    scope.spawn(move || {
                        let mut registration = threads::Registration::enter();
                        let live = object::new_value(Data::List(Vec::new(), ptr::null_mut(), ptr::null_mut()));
                        let cycle = object::new_value(Data::List(Vec::new(), ptr::null_mut(), ptr::null_mut()));
                        let Data::List(values, ..) = &mut (*cycle).data else { unreachable!() };
                        values.push(retain(cycle));
                        release(cycle);
                        let foreign = registration.foreign();
                        ready_send.send((live as usize, cycle as usize)).unwrap();
                        finish.recv().unwrap();
                        drop(foreign);
                        release(live);
                    });
                    let (live, cycle) = ready.recv().unwrap();
                    let live = retain(live as Value);
                    assert!(gc::is_tracked(live));
                    assert!(gc::is_tracked(cycle as Value));
                    let module = mule_namespace();
                    assert!(gc::populate(module));
                    let collect = object::lookup(module, "gc_collect");
                    let result = mule_call(collect, 0, ptr::null());
                    assert_eq!(result, small(1));
                    release(result);
                    assert!(!gc::is_tracked(cycle as Value));
                    assert!(gc::is_tracked(live));
                    assert_eq!((*live).reference_count(), 2);
                    release(module);
                    finish_send.send(()).unwrap();
                    release(live);
                });
                println!("shared heap=ok");
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
            "name_error" => return load_global(globals, c"absent_name".as_ptr()),
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
